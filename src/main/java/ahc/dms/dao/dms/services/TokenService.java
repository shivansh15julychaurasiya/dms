package ahc.dms.dao.dms.services;

import ahc.dms.payload.TokenDto;
import ahc.dms.dao.dms.entities.Token;
import ahc.dms.dao.dms.repositories.TokenRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TokenService {

    @Autowired
    private TokenRepository tokenRepository;
    @Autowired
    private ModelMapper modelMapper;
    private final Logger logger = LoggerFactory.getLogger(TokenService.class);

    @Transactional(transactionManager = "dmsTransactionManager")
    public TokenDto saveToken(TokenDto tokenDto) {
        logger.info("saving token!!!!");
        Token newToken = tokenRepository.save(modelMapper.map(tokenDto, Token.class));
        return modelMapper.map(newToken, TokenDto.class);
    }

    @Transactional(transactionManager = "dmsTransactionManager")
    public TokenDto revokeToken(Long tokenId) {
        Token token = tokenRepository.findById(tokenId)
                .orElseThrow(() -> new ResourceNotFoundException("Token", "Token Id", tokenId));
        token.setTokenStatus(false);
        Token deletedToken = tokenRepository.save(token);
        return modelMapper.map(deletedToken, TokenDto.class);

    }

    public TokenDto findTokenByLoginId(String loginId) {
        Token token = tokenRepository.findByLoginId(loginId);
        if (token != null)
            return modelMapper.map(token, TokenDto.class);
        return null;
    }

    public TokenDto findToken(String token, String loginId) {
        Token existingToken = tokenRepository.findByLoginIdAndJwtToken(loginId, token);
        if (existingToken != null)
            return modelMapper.map(existingToken, TokenDto.class);
        return null;
    }
}
