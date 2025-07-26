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
import java.util.Optional;

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

    public long getCourtMasterCount() {
        return courtMasterRepository.count(); // counts based on cm_id (primary key)
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

        System.out.println("----------------Court-master Id"+id+"/n"+"--------Bench Id:"+benchId);
        // 1. Find the court by its ID (the one to update)
        CourtMaster court = courtMasterRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Court", "id", id));

        // 2. Check if any other court already has the same benchId
        Optional<CourtMaster> existing = courtMasterRepository.findByCmBenchId(benchId);

        if (existing.isPresent()) {
            CourtMaster existingCourt = existing.get();
            // Make sure it's not the same record being updated
            if (!existingCourt.getCm_id().equals(id)) {
                existingCourt.setCmBenchId(null);
                courtMasterRepository.save(existingCourt); // Save the null update
            }
        }

        // 3. Now assign the benchId to the current court and save
        court.setCmBenchId(benchId);
        return courtMasterRepository.save(court);
    }





    public void deleteCourtMaster(Integer id) {
        courtMasterRepository.deleteById(id);
    }
}
