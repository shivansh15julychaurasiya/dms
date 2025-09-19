package ahc.dms.dao.dms.entities;

import java.util.Date;

import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
@Entity
@Table(name = "USER_ROLE")
public class UserRole {
	
	@Id
	@GeneratedValue (strategy = GenerationType.SEQUENCE, generator="user_role_seq")
	@SequenceGenerator(name="user_role_seq", sequenceName="user_role_seq", allocationSize=1)
	@Column(name = "ur_id")
	private Long ur_id;
	
	@Column(name = "ur_role_id")
	private Long ur_role_id;
	
	@Column(name = "ur_um_mid")
	private Long ur_um_mid;
	
	@Column(name = "ur_cr_by")
	private Long ur_cr_by;

	@Column(name = "ur_cr_date")
	private Date ur_cr_date;
	
	@Column(name = "ur_mod_by")
	private Long ur_mod_by;
	
	@Column(name = "ur_mod_date")
	private Date ur_mod_date;
	
	@Column(name = "ur_rec_status")
	private Integer ur_rec_status;
	
	
	


//	@OneToOne(cascade = CascadeType.PERSIST)
//    @JoinColumn(name = "ur_um_mid",insertable = false, updatable = false)
//	private User user;
	
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "ur_um_mid", referencedColumnName = "um_id", insertable = false, updatable = false)
	private User user;

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ur_role_id", referencedColumnName = "lk_id", insertable = false, updatable = false)
	private Lookup role;  // this replaces the old Role entity

	
	
	
}