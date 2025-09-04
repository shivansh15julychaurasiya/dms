package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "download_files")
@Data
@NoArgsConstructor
@AllArgsConstructor        
@Builder                   
public class DownloadFile {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "download_files_seq")
    @SequenceGenerator(name = "download_files_seq", sequenceName = "download_files_seq", allocationSize = 1)
    @Column(name = "df_id")
    private Long dfId;

    @Column(name = "df_dr_mid")
    private Long dfDrMid;

    @Column(name = "df_sd_mid")
    private Long dfSdMid;

    @Column(name = "df_ord_mid")
    private Long dfOrdMid;

    @Column(name = "df_submitted_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date dfSubmittedDate;

    @Column(name = "df_pages")
    private Integer dfPages;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "df_sd_mid", insertable = false, updatable = false)
    private SubDocument subDocument;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "df_ord_mid", insertable = false, updatable = false)
    private OrderReport ordeReport;
}
