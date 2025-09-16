package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.SubDocument;
import ahc.dms.dao.dms.repositories.SubDocumentRepository;
import jakarta.transaction.Transactional;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SubDocumentService {

    private final SubDocumentRepository subDocumentRepository;

    @Autowired
    public SubDocumentService(SubDocumentRepository subDocumentRepository) {
        this.subDocumentRepository = subDocumentRepository;
    }

    @Transactional
    public Optional<SubDocument> getPetitionSubDocument(Long fdId, Integer recStatus) {
    	
        return subDocumentRepository
                .findFirst(fdId,1,recStatus);
                
    }
}
