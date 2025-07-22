package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "court_master")
@Data
public class CourtMaster {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "court_master_seq")
    @SequenceGenerator(name = "court_master_seq", sequenceName = "court_master_seq", allocationSize = 1)
    @Column(name = "cm_id")
    private Integer cm_id;

    @Column(name = "cm_name")
    private String cm_name;

    @Column(name = "cm_value")
    private Integer cm_value;

    @Column(name = "cm_rec_status")
    private Integer cm_rec_status;

    @Column(name = "cm_bench_id")
    private Integer cm_bench_id;

    @Column(name = "cm_cr_by")
    private Long cm_cr_by;

    @Column(name = "cm_cr_date")
    private Date cm_cr_date;

    @Column(name = "cm_mod_by")
    private Long cm_mod_by;

    @Column(name = "cm_mod_date")
    private Date cm_mod_date;

    @Transient
    private boolean updateFlag = false;
}
