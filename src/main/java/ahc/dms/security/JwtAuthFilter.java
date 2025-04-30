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
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Arrays;

@Component
public class JwtAuthFilter extends OncePerRequestFilter {

    @Autowired
    private UserDetailsService userDetailsService;
    @Autowired
    private JwtTokenHelper jwtTokenHelper;
    private final Logger logger = LoggerFactory.getLogger(JwtAuthFilter.class);

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        return Arrays.stream(AppConstants.JWT_IGNORED_URLS)
                .anyMatch(url -> request.getRequestURI().equalsIgnoreCase(url)
                );
    }

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {

        // Only process JWT for non-excluded paths
        if (shouldNotFilter(request)) {
            logger.info("ignoring filter : JwtAuthFilter");
            filterChain.doFilter(request, response);
            return;
        }

        String authHeader = request.getHeader("Authorization");
        logger.info("Auth Header : {}", authHeader);

        String username = null;
        String token = null;

        //getting token
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            //Bearer afa87fasd89
            logger.info("Extracting Token");
            token = authHeader.substring(7);
            try {
                username = this.jwtTokenHelper.getUsernameFromToken(token);
            } catch (IllegalArgumentException e) {
                logger.info("Unable to get user");
            } catch (ExpiredJwtException e) {
                logger.info("JWT expired");
            } catch (MalformedJwtException e) {
                logger.info("Malformed JWT");
            }
        } else {
            logger.info("JWT doesn't start with Bearer");
        }

        //validating token
        if (username !=null && SecurityContextHolder.getContext().getAuthentication()==null) {
            UserDetails userDetails = this.userDetailsService.loadUserByUsername(username);
            if (this.jwtTokenHelper.validateToken(token, userDetails)) {
                //now set the authentication
                UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken =
                        new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                usernamePasswordAuthenticationToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);
            } else {
                logger.info("Invalid JWT");
            }
        } else {
            logger.info("User/Context is null");
        }
        filterChain.doFilter(request, response);
    }



}
