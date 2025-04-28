package ahc.dms.dao.pgdms2.services;

import ahc.dms.dao.pgdms2.entities.TokenLogs;
import ahc.dms.dao.pgdms2.repositories.TokenLogsRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.TokenDto;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TokenLogsService {

    @Autowired
    private TokenLogsRepository tokenLogsRepository;
    @Autowired
    private ModelMapper modelMapper;
    private final Logger logger = LoggerFactory.getLogger(TokenLogsService.class);

    @Transactional (transactionManager = "msDmsTransactionManager")
    public TokenDto saveToken(TokenDto tokenDto) {
        logger.info("saving token!!!!");
        TokenLogs newToken = tokenLogsRepository.save(modelMapper.map(tokenDto, TokenLogs.class));
        return modelMapper.map(newToken, TokenDto.class);
    }

    @Transactional (transactionManager = "msDmsTransactionManager")
    public TokenDto revokeToken(Long tokenId) {
        TokenLogs token = tokenLogsRepository.findById(tokenId)
                .orElseThrow(() -> new ResourceNotFoundException("Token", "Token Id", tokenId));
        token.setTokenStatus(false);
        TokenLogs deletedToken = tokenLogsRepository.save(token);
        return modelMapper.map(deletedToken, TokenDto.class);

    }

    public TokenDto findTokenByLoginId(String loginId) {
        TokenLogs token = tokenLogsRepository.findByLoginId(loginId);
        if (token != null)
            return modelMapper.map(token, TokenDto.class);
        return null;
    }

    public TokenDto findToken(String token, String loginId) {
        TokenLogs existingToken = tokenLogsRepository.findByLoginIdAndJwtToken(loginId, token);
        if (existingToken != null)
            return modelMapper.map(existingToken, TokenDto.class);
        return null;
    }
}
