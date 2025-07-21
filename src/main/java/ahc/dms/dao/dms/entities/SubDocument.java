package ahc.dms.dao.dms.entities;


import java.util.Date;
import java.util.List;


import jakarta.persistence.*;
import lombok.Data;
import org.hibernate.annotations.Where;

@Entity
@Table(name = "sub_documents")
@Data
public class SubDocument {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sub_documents_seq")
    @SequenceGenerator(name = "sub_documents_seq", sequenceName = "sub_documents_seq", allocationSize = 1)
    @Column(name = "sd_id")
    private Long sd_id;

    @Column(name = "sd_fd_mid")
    private Long sd_fd_mid;

    @Column(name = "sd_if_mid")
    private Long sd_if_mid;

    @Column(name = "sd_document_name")
    private String sd_document_name;

    @Column(name = "sd_application_status")
    private String sd_application_status;

    @Column(name = "sd_no_of_pages")
    private int sd_no_of_pages;

    @Column(name = "sd_version")
    private int sd_version;

    @Column(name = "sd_document_id")
    private Integer sd_document_id;

    @Column(name = "sd_document_no")
    private Integer sd_document_no;

    @Column(name = "sd_document_year")
    private Integer sd_document_year;

    @Column(name = "sd_party")
    private String sd_party;

    @Column(name = "sd_description")
    private String sd_description;

    @Column(name = "sd_submitted_date")
    private Date sd_submitted_date;

    @Column(name = "sd_cr_by")
    private Long sd_cr_by;

    @Column(name = "sd_cr_date")
    private Date sd_cr_date;

    @Column(name = "sd_rec_status")
    private int sd_rec_status;

    @Column(name = "sd_major_sequence")
    private int sd_major_sequence;

    @Column(name = "sd_minor_sequence")
    private int sd_minor_sequence;

    @Column(name = "sd_judgement_id")
    private Long sd_judgement_id;

    @Column(name = "sd_counsel")
    private String sd_counsel;

    @Column(name = "sd_status")
    private Long sd_status;

    @Column(name = "sd_date")
    private Date sd_date;

    @Column(name = "sd_mod_by")
    private Long sd_mod_by;

    @Column(name = "sd_mod_date")
    private Date sd_mod_date;

    @Column(name = "sd_nonmaintainable")
    private boolean sd_nonmaintainable;

    @Column(name = "checked")
    private boolean checked;

    @Column(name = "pdf_highlight_mode")
    private Boolean pdf_highlight_mode;

    @Column(name = "edited_by")
    private Long edited_by;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "sd_if_mid",insertable = false, updatable = false)
    private IndexField indexField;

//    @OneToOne(cascade = CascadeType.PERSIST)
//    @JoinColumn(name = "sd_document_id",insertable = false, updatable = false)
//    private ApplicationTypes documentType;


    @Transient
    private Long judgmentID;

    @Transient
    private boolean checkBoxValue;


}
