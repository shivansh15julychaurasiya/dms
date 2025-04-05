package ahc.dms.dao.services;

import ahc.dms.payload.TokenDto;
import ahc.dms.dao.entities.Token;
import ahc.dms.dao.respositories.TokenRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import jakarta.transaction.Transactional;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TokenService {

    @Autowired
    private TokenRepository tokenRepository;
    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public TokenDto saveToken(TokenDto tokenDto) {
        Token newToken = tokenRepository.save(modelMapper.map(tokenDto, Token.class));
        return modelMapper.map(newToken, TokenDto.class);
    }

    @Transactional
    public TokenDto revokeToken(Long tokenId) {
        Token token = tokenRepository.findById(tokenId)
                .orElseThrow(() -> new ResourceNotFoundException("Token", "Token Id", tokenId));
        token.setTokenStatus(false);
        Token deletedToken = tokenRepository.save(token);
        return modelMapper.map(deletedToken, TokenDto.class);

    }

    public TokenDto findTokenByUsername(String username) {
        Token token = tokenRepository.findByUsername(username);
        if (token != null)
            return modelMapper.map(token, TokenDto.class);
        return null;
    }

    public TokenDto findToken(String token, String username) {
        Token existingToken = tokenRepository.findByUsernameAndJwtToken(username, token);
        if (existingToken != null)
            return modelMapper.map(existingToken, TokenDto.class);
        return null;
    }
}
