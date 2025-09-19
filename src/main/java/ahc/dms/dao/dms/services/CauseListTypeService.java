package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.repositories.CauseListTypeRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Optional;

@Service
public class CauseListTypeService{
	
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************

    @Autowired
    private CauseListTypeRepository repository;


    public CauseListType create(CauseListType causeListType) {
        causeListType.setClt_cr_date(new Date());
        return repository.save(causeListType);
    }


    public List<CauseListType> getAll() {
        return repository.findAll();
    }


    public Optional<CauseListType> getById(Long id) {
        return repository.findById(id);
    }


    public CauseListType update(Long id, CauseListType updated) {
        CauseListType existing = repository.findById(id)
                .orElseThrow(() -> new RuntimeException("CauseListType not found with ID: " + id));
        existing.setClt_name(updated.getClt_name());
        existing.setClt_rec_status(updated.getClt_rec_status());
        existing.setClt_mod_by(updated.getClt_mod_by());
        existing.setClt_mod_date(new Date());
        existing.setClt_description(updated.getClt_description());
        return repository.save(existing);
    }



}
