package ahc.dms.security;

import ahc.dms.config.AppConstants;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class JwtAuthFilter extends OncePerRequestFilter {


//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
    @Autowired
    private UserDetailsService userDetailsService;

    @Autowired
    private JwtHelper jwtHelper;

    private final Logger logger = LoggerFactory.getLogger(JwtAuthFilter.class);
    private static final PathMatcher PATH_MATCHER = new AntPathMatcher();

    @Override
    protected boolean shouldNotFilter(@NonNull HttpServletRequest request) {
        String uri = request.getRequestURI();
        logger.info("Checking JWT_IGNORED_URLS for : {}", uri);
        return AppConstants.JWT_IGNORED_URLS.stream()
                .anyMatch(pattern -> PATH_MATCHER.match(pattern, uri));
    }

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull FilterChain filterChain) throws ServletException, IOException {

        logger.info("Requested URI: {}, Method: {}", request.getRequestURI(), request.getMethod());

        String authHeader = request.getHeader("Authorization");
        logger.info("Auth Header : {}", authHeader);

        String username = null;
        String token = null;
        String tokenType = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            token = authHeader.substring(7);
            tokenType = jwtHelper.getTokenTypeFromToken(token);
            try {
                username = jwtHelper.getUsernameFromToken(token);
            } catch (IllegalArgumentException e) {
                logger.warn("Unable to get JWT Token");
            } catch (ExpiredJwtException e) {
                logger.warn("JWT Token has expired");
            } catch (MalformedJwtException e) {
                logger.warn("JWT Token is malformed");
            }
        } else {
            logger.warn("JWT Token does not begin with Bearer");
        }

        logger.info("Username = {}", username);

        if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            UserDetails userDetails = userDetailsService.loadUserByUsername(username);
            if (jwtHelper.validateToken(token, tokenType, userDetails)) {
            	
                UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                        userDetails, null, userDetails.getAuthorities()
                );
                authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(authToken);
                logger.info("User authenticated: {}", username);
            } else {
                logger.warn("Invalid JWT token");
            }
        } else {
            logger.info("Username is null or already authenticated");
        }

        filterChain.doFilter(request, response);
    }
}
