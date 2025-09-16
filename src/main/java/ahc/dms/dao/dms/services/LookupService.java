package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.Lookup;
import ahc.dms.dao.dms.repositories.LookupRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class LookupService {

    private final LookupRepository lookupRepository;

    public LookupService(LookupRepository lookupRepository) {
        this.lookupRepository = lookupRepository;
    }

    @Transactional
    public Lookup getLookUpObject(String setname) {
        try {
            Optional<Lookup> result = lookupRepository.findBySetnameAndRecStatus(setname, 1);
            return result.orElse(null);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
