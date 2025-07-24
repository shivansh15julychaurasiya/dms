package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CourtMaster;
import ahc.dms.dao.dms.repositories.CourtMasterRepository;
import ahc.dms.dao.dms.services.CourtMasterService;
import ahc.dms.exceptions.ResourceNotFoundException;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
public class  CourtMasterService {

    @Autowired
    private CourtMasterRepository courtMasterRepository;


    public CourtMaster saveCourtMaster(CourtMaster courtMaster) {
        courtMaster.setCm_cr_date(new Date());
        return courtMasterRepository.save(courtMaster);
    }


    public List<CourtMaster> getAllCourtMasters() {
        return courtMasterRepository.findAll();
    }

//    public CourtMaster updateBenchId(Integer courtId, Integer benchId) {
//        CourtMaster court = courtMasterRepository.findById(courtId)
//                .orElseThrow(() -> new RuntimeException("CourtMaster not found with id: " + courtId));
//
//        court.setCm_bench_id(benchId);
//        court.setCm_mod_date(new Date()); // update last modified date (optional)
//
//        return courtMasterRepository.save(court);
//    }

    @Transactional
    public CourtMaster updateBenchId(Integer id, Integer benchId) {
        CourtMaster court = courtMasterRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Court", "id", id));

        court.setCm_bench_id(benchId);
        return courtMasterRepository.save(court);
    }





    public void deleteCourtMaster(Integer id) {
        courtMasterRepository.deleteById(id);
    }
}
