package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.entities.SubDocument;
import ahc.dms.dao.dms.repositories.CaseFileDetailsRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Optional;
//import org.apache.pdfbox.pdmodel.PDDocument;



@Service
@RequiredArgsConstructor
public class CaseFileDetailsService {
    private final CaseFileDetailsRepository repository;

    public List<CaseFileDetails> searchCases(Integer caseTypeId, String caseNo, Integer caseYear) {
        System.out.println("From Service-> Casetype="+caseTypeId+" "+"CaseNO="+caseNo+" "+"CaseYear="+caseYear);
        return repository.searchCases(caseTypeId, caseNo, caseYear);
    }

    public Optional<CaseFileDetails> getCaseFileDetail(Long docId) {
        return repository.findById(docId);
    }


    private static final String BASE_PATH = "A:/pdfpath/"; // change as needed
    public byte[] getTop10PagesPdf(String caseType, String caseNo, Integer caseYear) throws Exception {
        CaseFileDetails casePdf = repository
                .findByCaseTypeAndCaseNoAndCaseYear(caseType, caseNo, caseYear)
                .orElseThrow(() -> new FileNotFoundException("PDF not found"));

        String fileName = caseType + "_" + caseNo + "_" + caseYear + "_" + "PETN_1";

        System.out.println("fdilename "+fileName);

        File file = new File(BASE_PATH + fileName +".pdf");

        if (!file.exists()) {
            throw new FileNotFoundException("PDF file not found at: " + file.getAbsolutePath());
        }

//        try (PDDocument document = PDDocument.load(file)) {
//            PDDocument newDoc = new PDDocument();
//
//            int pageCount = Math.min(10, document.getSd_no_of_pages());
//
//            for (int i = 0; i < pageCount; i++) {
//                newDoc.addPage(document.getPage(i));
//            }
//
//            ByteArrayOutputStream out = new ByteArrayOutputStream();
//            newDoc.save(out);
//            newDoc.close();
//            return out.toByteArray();
//        }
        return "this is return data".getBytes();
    }





}
