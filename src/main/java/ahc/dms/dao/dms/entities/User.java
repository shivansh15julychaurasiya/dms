package ahc.dms.dao.dms.entities;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "user_master")
@Getter
@Setter
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class User implements UserDetails {

//	  ********************** JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
	
	@Version
	private Long version;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_master_seq")
	@SequenceGenerator(name = "user_master_seq", sequenceName = "user_master_seq", allocationSize = 1)
	@Column(name = "um_id")
	private Long umId;

	@Column(name = "um_username", nullable = false, unique = true)
	private String username;

	@Column(name = "um_password", nullable = false)
	private String password;

	@Column(name = "um_last_login")
	private Date last_login;

	@CreatedBy
	@Column(name = "um_cr_by", nullable = false, updatable = false)
	private Long cr_by;

	@CreatedDate
	@Column(name = "um_cr_date", nullable = false, updatable = false)
	private Date cr_date;

	@Column(name = "um_mod_by")
	private Long mod_by;

	@Column(name = "um_mod_date")
	private Date mod_date;

	@Column(name = "um_fullname")
	private String um_fullname;

	@Column(name = "um_mobile")
	private String umMobile;
	
	@Column(name = "um_ipaddress")
	private String um_ipaddress;

	@Column(name = "um_pass_validity_date")
	private Date um_pass_validity_date;

	@Column(name = "um_rec_status", nullable = false)
	private Integer rec_status = 1;

	@Column(name = "um_department_id")
	private Long um_department_id;

	// Transient / DTO fields
	@Transient
	private List<Long> um_ct_id;

	@Transient
	private String old_password;

	@Transient
	private Long um_role_id;

	@Transient
	private Integer court_id;

	@Transient
	private String confirmpassword;

	@Transient
	private CourtMaster courtMaster;

	// New fields
	@Column(name = "name", length = 100)
	private String name;

	@Column(name = "email", length = 100, unique = true)
	private String email;

	@Column(name = "phone", length = 100, unique = true)
	private String phone;

	@Column(name = "about")
	private String about;

	@Column(name = "status", nullable = false, columnDefinition = "boolean default true")
	private Boolean status = true;

	// Audit fields
	@CreationTimestamp
	@Column(name = "created_at", updatable = false)
	private LocalDateTime createdAt;

	@UpdateTimestamp
	@Column(name = "updated_at")
	private LocalDateTime updatedAt;

	@CreatedBy
	@Column(name = "created_by", updatable = false)
	private String createdBy;

	@LastModifiedBy
	@Column(name = "updated_by")
	private String updatedBy;

	// === Relationships ===
	@OneToMany(mappedBy = "user", cascade = CascadeType.ALL, fetch = FetchType.EAGER, orphanRemoval = true)
	private List<UserRole> userRoles = new ArrayList<>();

	// === Security (Spring UserDetails) ===

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		if (this.userRoles == null || this.userRoles.isEmpty()) {
			System.out.println("********* No roles for user=" + this.username);
			return Collections.emptyList();
		}

		return this.userRoles.stream().filter(userRole -> userRole.getUr_rec_status() != null
				&& userRole.getUr_rec_status() == 1 && userRole.getRole() != null).map(userRole -> {
					String roleName = userRole.getRole().getLongname();
					System.out.println(">>> Mapping role to authority=" + roleName);
					return new SimpleGrantedAuthority(roleName);
				}).collect(Collectors.toList());
	}

	@Override
	public String getUsername() {
		return this.username;
	}

	@Override
	public boolean isAccountNonExpired() {
		return true;
	}

	@Override
	public boolean isAccountNonLocked() {
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}

	@Override
	public boolean isEnabled() {
		return Boolean.TRUE.equals(this.status);
	}

	// === Helper Methods ===
//    public void deactivateAllUserRoles() {
//        this.userRoles.forEach(ur -> ur.setStatus(false));
//    }
//
//    public Optional<Role> getActiveUserRole() {
//        return this.userRoles.stream()
//                .filter(UserRole::getStatus)
//                .map(UserRole::getRole)
//                .findFirst();
//    }
}
