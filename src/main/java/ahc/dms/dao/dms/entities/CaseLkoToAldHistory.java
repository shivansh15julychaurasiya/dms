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
@Table(name = "casemove_lkotoald_history")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CaseLkoToAldHistory {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "lkotoald_seq")
    @SequenceGenerator(name = "lkotoald_seq", sequenceName = "lkotoald_seq", allocationSize = 1)
    @Column(name = "lko_to_ald_id")
    private Long lko_to_ald_id;

    private Long old_fd_mid;
    private Integer old_case_type;
    private String old_case_no;
    private Integer old_case_year;

    private Long new_fd_mid;
    private Integer new_case_type;
    private String new_case_no;
    private Integer new_case_year;

    private Long user_cr_by;

    @Temporal(TemporalType.TIMESTAMP)
    private Date lkotoald_cr_date;

    private Long user_mod_by;

    @Temporal(TemporalType.TIMESTAMP)
    private Date lkotoald_mod_date;
}
