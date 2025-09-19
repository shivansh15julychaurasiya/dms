package ahc.dms.dao.dms.services;

import java.util.List;

import org.springframework.stereotype.Service;

import ahc.dms.dao.dms.entities.Lookup;
import ahc.dms.dao.dms.repositories.LookupRepository;
import jakarta.transaction.Transactional;

@Service
public class LookupService {
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************

    private final LookupRepository lookupRepository;

    public LookupService(LookupRepository lookupRepository) {
        this.lookupRepository = lookupRepository;
    }

    @Transactional
    public Lookup getLookUpObject(String setname) {
        try {
            List<Lookup> result = lookupRepository.findListBySetnameAndRecStatus(setname, 1);
            return result.stream().findFirst().orElse(null);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
