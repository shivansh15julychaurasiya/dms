package ahc.dms.dao.dms.services;


import ahc.dms.dao.dms.entities.CauseList;
import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.entities.CourtUserMapping;
import ahc.dms.dao.dms.repositories.CauseListRepository;
import ahc.dms.dao.dms.repositories.CourtUserMappingRepository;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;


@Service
public class CauseListService {

    @Autowired
    private CauseListRepository causeListRepository;
    @Autowired
    private CourtUserMappingRepository courtUserMappingRepository;

    public List<CauseList> searchCauseLists(Integer courtNo, Long listTypeId, LocalDate dol) {
        return causeListRepository.searchCauseLists(courtNo, listTypeId, dol);
    }

    public CauseListType searchListType(Long listTypeId) {
        return causeListRepository.searchListType(listTypeId);
    }

    public CourtUserMapping getCourtMapping(Long userId){
        return courtUserMappingRepository.getCourtMapping(userId);
    }

    public List<CauseList> getCauseList(CauseList causeList) {
            LocalDate clDol = LocalDate.now();
            Integer clCourtNo = causeList.getCl_court_no();
            Long clListTypeMid = causeList.getCl_list_type_mid();
         return causeListRepository.getCauseLists(clDol,clCourtNo,clListTypeMid);
    }

//    public List<CauseList> getListByType(CauseList causeList){
//   LocalDate dol = causeList.getCl_dol();
//    System.out.println("Date format ===="+dol);
//        Integer courtNo = causeList.getCl_court_no();
//        return causeListRepository.getByListType(dol,courtNo);
//    }
public List<Object[]> getListByType(CauseList causeList) {
    LocalDate dol = causeList.getCl_dol();
    Integer courtNo = causeList.getCl_court_no();

    if (dol == null || courtNo == null) {
        throw new IllegalArgumentException("Date of listing and Court number must not be null.");
    }

    System.out.println("Date format ====" + dol);
    System.out.println("Court No ====" + courtNo);

    List<Object[]> result = causeListRepository.getByListType(dol, courtNo);
    System.out.println("Fetched rows count: " + result.size());

    return result;
}





}

