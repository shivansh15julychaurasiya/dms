package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CourtMaster;
import ahc.dms.dao.dms.repositories.CaseTypeRepository;
import ahc.dms.dao.dms.repositories.CourtMasterRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.List;
@Service
public class CourtMasterService {

    @Autowired
    private CourtMasterRepository courtMaster;

    public List<CourtMaster> getCourtMaster(){
        System.out.println("Print data in data ------" +courtMaster);
        return courtMaster.findAll(Sort.by("cmId").ascending());
    }
}
