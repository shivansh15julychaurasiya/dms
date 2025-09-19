package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.Documents;
import ahc.dms.dao.dms.repositories.DocumentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.*;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
public class DocumentService {

    @Value("${document.upload-dir}")
    private String uploadDir;

    @Autowired
    private DocumentRepository documentRepository;

    //  Upload Document
    public Documents uploadDocument(MultipartFile file,
                                    String title,
                                    String fileType,
                                    String caseType,
                                    String caseNumber,
                                    String caseYear,
                                    String uploadDate,
                                    String uploadedBy) throws IOException {

        String storedFileName = UUID.randomUUID() + "_" + file.getOriginalFilename();
        Path destination = Paths.get(uploadDir).resolve(storedFileName);

        Files.createDirectories(destination.getParent());
        Files.write(destination, file.getBytes());

        Documents doc = new Documents();
        doc.setFileName(file.getOriginalFilename());
        doc.setFileType(fileType);
        doc.setTitle(title);
        doc.setCaseType(caseType);
        doc.setCaseNumber(caseNumber);
        doc.setCaseYear(caseYear);
        doc.setUploadDate(LocalDate.parse(uploadDate));
        doc.setUploadedBy(uploadedBy);
        doc.setFilePath(destination.toString());


        return documentRepository.save(doc);
    }

    //  Search Documents by caseTypeLabel, caseNo, caseYear
    public List<Documents> searchDocuments(String caseTypeLabel, String caseNo, String caseYear) {
        System.out.println("Searching for: " + caseTypeLabel + ", " + caseNo + ", " + caseYear);
        return documentRepository.searchDocuments(caseTypeLabel, caseNo, caseYear);
    }
}
