package ahc.dms.dao.dms.entities;



import java.util.Date;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "order_report_data")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrderReport {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ord_seq")
    @SequenceGenerator(name = "ord_seq", sequenceName = "ord_seq", allocationSize = 1)
    @Column(name = "ord_id")
    private Long ord_id;

    @Column(name = "ord_remark")
    private String ord_remark;

    @Transient
    private Integer cl_flag = 0;

    @Column(name = "ord_created")
    private Date ord_created;

    @Column(name = "ord_created_by")
    private Long ord_created_by;

    @Column(name = "ord_fd_mid")
    private Long ordFdMid;

    @Column(name = "ord_sd_mid")
    private Long ordSdMid;


    @Column(name = "ord_rec_status")
    private Integer ord_rec_status;

    @Column(name = "ord_mod_by")
    private Long ord_mod_by;

    @Column(name = "ord_mod_date")
    private Date ord_mod_date;

    @Column(name = "ord_consignment_no")
    private String ord_consignment_no;

    @Column(name = "ord_submitted_date")
    private Date ord_submitted_date;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "ord_sd_mid", insertable = false, updatable = false)
    private SubDocument subDocument;
    
	/*
	 * @ManyToOne
	 * 
	 * @JoinColumn(name = "ord_created_by", insertable = false, updatable = false)
	 * private User createdByUser;
	 */

    

    @Transient
    private boolean checked;
}
