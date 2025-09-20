package ahc.dms.dao.dms.entities;

import java.time.LocalDateTime;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Table(name = "lookup")
@Data
public class Lookup {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "lookup_seq")
    @SequenceGenerator(
        name = "lookup_seq",
        sequenceName = "lookup_SEQ", // must match your DB sequence name
        allocationSize = 1           // must match DB increment (your DB uses 1)
    )
    @Column(name = "lk_id")
    private Long lkId;

    @Column(name = "lk_longname")
    private String longname;

    @Column(name = "lk_setname")
    private String setname;

    @Column(name = "lk_value")
    private String value;

    @Column(name = "lk_parent")
    private Long parent;

    @Column(name = "lk_rec_status")
    private Integer recStatus;

    @Column(name = "lk_serial_no")
    private Long serialNo;

    @Column(name = "lk_cr_by")
    private Long createdBy;

    @Column(name = "lk_cr_date")
    private LocalDateTime createdDate;

    @Column(name = "lk_mod_by")
    private Long modifiedBy;

    @Column(name = "lk_mod_date")
    private LocalDateTime modifiedDate;

    @Column(name = "lk_priority")
    private Integer priority;
}
