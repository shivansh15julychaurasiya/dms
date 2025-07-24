package ahc.dms.dao.dms.services;


import ahc.dms.dao.dms.entities.CauseList;
import ahc.dms.dao.dms.repositories.CauseListRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
public class CauseListService {



    @Autowired
    private CauseListRepository causeListRepository;

    public List<CauseList> searchCauseLists(String courtName, String desc, Date date) {
        return causeListRepository.searchByCourtNameAndListTypeDescAndDate(courtName, desc, date);
    }
}
