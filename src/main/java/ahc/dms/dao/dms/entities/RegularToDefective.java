package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "regular_defective_history")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class RegularToDefective {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "rdh_seq")
    @SequenceGenerator(name = "rdh_seq", sequenceName = "rdh_seq", allocationSize = 1)
    @Column(name = "rdh_id")
    private Long rdh_id;

    private Long rdh_fd_mid_old;
    private Long rdh_fd_mid_new;

    private String rdh_case_type_old;
    private String rdh_case_no_old;
    private String rdh_case_year_old;

    private String rdh_case_type;
    private String rdh_case_no;
    private String rdh_case_year;

    private Long rdh_cr_by;

    @Temporal(TemporalType.TIMESTAMP)
    private Date rdh_cr_date;

    private Long rdh_mod_by;

    @Temporal(TemporalType.TIMESTAMP)
    private Date rdh_mod_date;
}
