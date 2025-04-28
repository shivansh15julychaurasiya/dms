package ahc.dms.security;

import ahc.dms.config.AppConstants;
import ahc.dms.payload.TokenDto;
import ahc.dms.dao.dms.services.TokenService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import jakarta.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

@Component
public class JwtTokenHelper {

    @Autowired
    private TokenService tokenService;
    private SecretKey key;
    private final Logger logger = LoggerFactory.getLogger(JwtTokenHelper.class);

    // Initializes the key after the class is instantiated and the jwtSecret is injected,
    // preventing the repeated creation of the key and enhancing performance
    @PostConstruct
    public void init() {
        this.key = Keys.hmacShaKeyFor(AppConstants.JWT_SECRET.getBytes(StandardCharsets.UTF_8));
    }

    //GENERATE TOKEN FOR USER
    public String generateToken(UserDetails userDetails) {

        logger.info("user details object = {}", userDetails);
        Map<String, Object> claims = new HashMap<>();
        Date expiration = new Date(System.currentTimeMillis() + AppConstants.JWT_TOKEN_VALIDITY);
        logger.info("Token will expire at {}", expiration);
        String token = Jwts.builder()
                .claims(claims)
                .subject(userDetails.getUsername())
                .issuedAt(new Date(System.currentTimeMillis()))
                .expiration(expiration)
                .signWith(key)
                .compact();

        logger.info("token : {}", token);
        TokenDto existingTokenDto = tokenService.findTokenByLoginId(userDetails.getUsername());
        if (existingTokenDto!=null) {
            logger.info("renewing token!!!");
            existingTokenDto.setJwtToken(token);
            existingTokenDto.setTokenStatus(true);
            tokenService.saveToken(existingTokenDto);
        } else {
            logger.info("creating new token!!!");
            TokenDto tokenDto = new TokenDto();
            tokenDto.setJwtToken(token);
            tokenDto.setLoginId(userDetails.getUsername());
            tokenDto.setExpirationDate(expiration);
            tokenDto.setTokenStatus(true);
            tokenService.saveToken(tokenDto);
        }
        logger.info("returning token!!!!");
        return token;
    }

    //VALIDATE TOKEN
    public Boolean validateToken(String token, UserDetails userDetails) {
        final String username = getUsernameFromToken(token);
        //check username and token expiry
        boolean validToken = (username.equals(userDetails.getUsername()) && !isTokenExpired(token));
        //if toke is valid
        if (validToken) {
            //check token status from db
            TokenDto existingToken = tokenService.findToken(token, userDetails.getUsername());
            return (existingToken != null) && existingToken.getTokenStatus();
        }
        return false;
    }

    //GET ALL CLAIM FROM TOKEN
    private Claims getAllClaimsFromToken(String token) {
        return Jwts.parser()
                .verifyWith(key)
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    //GET SINGLE CLAIM FROM TOKEN
    private <T> T getClaimFromToken(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = getAllClaimsFromToken(token);
        return claimsResolver.apply(claims);
    }

    //GET USERNAME FROM TOKEN
    public String getUsernameFromToken(String token) {
        return getClaimFromToken(token, Claims::getSubject);
    }

    //CHECKS TOKEN EXPIRY
    private boolean isTokenExpired(String token) {
        final Date expiry = getClaimFromToken(token, Claims::getExpiration);
        return expiry.before(new Date());
    }

}
