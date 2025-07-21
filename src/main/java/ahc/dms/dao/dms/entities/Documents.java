package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Entity
@Table(name = "documents")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Documents {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name="file_name")
    private String fileName;
    @Column(name="file_type")
    private String fileType;

    private String title;

    @Column(name = "case_type")
    private String caseType;



    @Column(name = "case_number")
    private String caseNumber;

    @Column(name = "case_year")
    private String caseYear;

    @Column(name = "upload_date")
    private LocalDate uploadDate;

    @Column(name = "uploaded_by")
    private String uploadedBy;

    @Column(name = "file_path")
    private String filePath;
}
