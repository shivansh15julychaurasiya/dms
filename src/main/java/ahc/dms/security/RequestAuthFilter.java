package ahc.dms.security;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.dms.entities.ObjectMaster;
import ahc.dms.dao.dms.entities.ObjectRole;
import ahc.dms.dao.dms.repositories.ObjectMasterRepository;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Arrays;
import java.util.Optional;
import java.util.Set;

@Component
public class RequestAuthFilter extends OncePerRequestFilter {

    @Autowired
    private ObjectMasterRepository objectMasterRepository;
    private final Logger logger = LoggerFactory.getLogger(RequestAuthFilter.class);

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        return Arrays.stream(AppConstants.REQUEST_AUTH_IGNORED_URLS)
                .anyMatch(url -> request.getRequestURI().equalsIgnoreCase(url)
        );
    }
    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        // Only process RequestAuth for non-excluded paths
        if (shouldNotFilter(request)) {
            logger.info("ignoring filter : RequestAuthFilter");
            filterChain.doFilter(request, response);
            return;
        }

        String uri = request.getRequestURI();
        String method = request.getMethod();

        Authentication auth = SecurityContextHolder.getContext().getAuthentication();

        // If not authenticated yet, continue — let Spring Security handle it
        if (auth == null || !auth.isAuthenticated()) {
            logger.info("user is not yet authenticated????");
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized");
            return;
        }

        // Get user roles
        String authRole = auth.getAuthorities()
                .stream()
                .map(GrantedAuthority::getAuthority)
                .findFirst()
                .orElse(null);
        logger.info("auth role : {}", authRole);

        Optional<ObjectMaster> objectMaster = objectMasterRepository.findByRequestUriAndRequestMethodAndStatusTrue(uri, method);

        if (objectMaster.isPresent()) {
            Set<ObjectRole> objectRoles = objectMaster.get().getObjectRoles();
            boolean roleMatched = objectRoles
                    .stream()
                    .anyMatch(objectRole -> {
                        String roleName = objectRole.getRole().getRoleName();
                        Boolean roleStatus = objectRole.getRole().getStatus();
                        Boolean objectRoleStatus = objectRole.getStatus();

                        return Boolean.TRUE.equals(roleStatus) &&
                                Boolean.TRUE.equals(objectRoleStatus) &&
                                roleName.equalsIgnoreCase(authRole);
                    });

            logger.info("Boolean : {}", roleMatched);

            if (roleMatched) {
                filterChain.doFilter(request, response);
            } else {
                logger.info("Access Denied");
                response.sendError(HttpServletResponse.SC_FORBIDDEN, "Access Denied");
            }
        } else {
            logger.info("API Not Registered");
            response.sendError(HttpServletResponse.SC_NOT_FOUND, "API Not Registered");
        }
        filterChain.doFilter(request, response);
    }
}
