package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CourtMaster;
import ahc.dms.dao.dms.repositories.CourtMasterRepository;
import ahc.dms.dao.dms.services.CourtMasterService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
public class  CourtMasterService {

    @Autowired
    private CourtMasterRepository repository;


    public CourtMaster saveCourtMaster(CourtMaster courtMaster) {
        courtMaster.setCm_cr_date(new Date());
        return repository.save(courtMaster);
    }


    public List<CourtMaster> getAllCourtMasters() {
        return repository.findAll();
    }

    public CourtMaster getCourtMasterById(Integer id) {
        return repository.findById(id).orElse(null);
    }


    public void deleteCourtMaster(Integer id) {
        repository.deleteById(id);
    }
}
