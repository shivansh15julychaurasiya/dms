package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.repositories.CaseFileDetailsRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CaseFileDetailsService {
    private final CaseFileDetailsRepository repository;

    public List<CaseFileDetails> searchCases(Integer caseTypeId, String caseNo, Integer caseYear) {
        return repository.searchCases(caseTypeId, caseNo, caseYear);
    }

    public CaseFileDetails getCaseFile(Long id) {
        return repository.findById(id).orElseThrow(() -> new RuntimeException("Case not found"));
    }
}
