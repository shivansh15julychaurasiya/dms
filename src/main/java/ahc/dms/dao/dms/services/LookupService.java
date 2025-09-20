package ahc.dms.dao.dms.services;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import ahc.dms.dao.dms.entities.Lookup;
import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.repositories.LookupRepository;
import ahc.dms.payload.dto.LookupDto;
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
    
    
    public LookupDto createRole(LookupDto dto) {
        // Check if role already exists
    	String ROLE_SETNAME="DMS_ROLE";
        if (lookupRepository.existsByLongnameAndSetname(ROLE_SETNAME, dto.getLkLongname())) {
            throw new RuntimeException("Role already exists: " + dto.getLkLongname());
        }
        
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        User userDetails = (User) authentication.getPrincipal();
        Lookup role = new Lookup();
    
        role.setLongname(dto.getLkLongname());
        role.setSetname(ROLE_SETNAME);
        role.setParent(0L);
        role.setRecStatus(dto.getStatus() != null ? dto.getStatus() : 1);
        role.setCreatedBy(userDetails.getUmId());
        role.setCreatedDate(LocalDateTime.now());

        Lookup saved = lookupRepository.save(role);

        LookupDto result = new LookupDto();
        result.setLkId(saved.getLkId());
        result.setLkLongname(saved.getLongname());
        result.setSetname(saved.getSetname());
        result.setStatus(saved.getRecStatus());
        result.setCreatedAt(saved.getCreatedDate());
        result.setUpdatedAt(saved.getModifiedDate());

        return result;
    }
    
    
}
