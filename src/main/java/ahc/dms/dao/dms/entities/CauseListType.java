package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "cause_list_type")
@Data
public class CauseListType {

	@Id
	@SequenceGenerator(
	    name = "cause_list_type_seq",            // internal Hibernate name
	    sequenceName = "cause_list_type_SEQ",    // actual DB sequence name
	    allocationSize = 1                       // must match DB sequence increment
	)
	@GeneratedValue(
	    strategy = GenerationType.SEQUENCE,
	    generator = "cause_list_type_seq"
	)
	@Column(name = "clt_id")
	private Long clt_id;


    @Column(name = "clt_name")
    private String clt_name;

    @Column(name = "clt_rec_status")
    private Integer clt_rec_status;

    @Column(name = "clt_cr_by")
    private Long clt_cr_by;

    @Column(name = "clt_cr_date")
    private Date clt_cr_date;

    @Column(name = "clt_mod_by")
    private Long clt_mod_by;

    @Column(name = "clt_mod_date")
    private Date clt_mod_date;

    @Column(name = "clt_description")
    private String clt_description;
}
