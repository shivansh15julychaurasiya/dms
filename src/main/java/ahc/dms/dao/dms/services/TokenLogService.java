package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.TokenLog;
import ahc.dms.payload.TokenDto;
import ahc.dms.dao.dms.repositories.TokenLogRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TokenLogService {

    @Autowired
    private TokenLogRepository tokenLogRepository;
    @Autowired
    private ModelMapper modelMapper;
    private final Logger logger = LoggerFactory.getLogger(TokenLogService.class);

    @Transactional(transactionManager = "dmsTransactionManager")
    public TokenDto saveToken(TokenDto tokenDto) {
        logger.info("saving token!!!!");
        TokenLog newTokenLog = tokenLogRepository.save(modelMapper.map(tokenDto, TokenLog.class));
        return modelMapper.map(newTokenLog, TokenDto.class);
    }

    @Transactional(transactionManager = "dmsTransactionManager")
    public TokenDto revokeToken(Long tokenId) {
        TokenLog tokenLog = tokenLogRepository.findById(tokenId)
                .orElseThrow(() -> new ResourceNotFoundException("Token", "Token Id", tokenId));
        tokenLog.setTokenStatus(false);
        TokenLog deletedTokenLog = tokenLogRepository.save(tokenLog);
        return modelMapper.map(deletedTokenLog, TokenDto.class);

    }

    public TokenDto getTokenByUsername(String username) {
        TokenLog tokenLog = tokenLogRepository.findByUsername(username);
        if (tokenLog != null)
            return modelMapper.map(tokenLog, TokenDto.class);
        return null;
    }

    public TokenDto getToken(String token, String username) {
        TokenLog existingTokenLog = tokenLogRepository.findByUsernameAndJwToken(username, token);
        if (existingTokenLog != null)
            return modelMapper.map(existingTokenLog, TokenDto.class);
        return null;
    }
}
