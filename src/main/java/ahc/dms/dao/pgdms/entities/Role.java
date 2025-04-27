package ahc.dms.dao.pgdms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "roles")
@Getter
@Setter
public class Role {

    @Id
    @Column(name = "role_id")
    private Integer roleId;
    @Column(name = "role_name", unique = true, nullable = false)
    private String roleName;
    @Column(nullable = false)
    private Boolean status = true;

    // Optional reverse mapping
//    @OneToMany(mappedBy = "role")
//    private Set<UserRole> userRoles = new HashSet<>();

    @PrePersist
    @PreUpdate
    public void capitalizeRoleName() {
        if (this.roleName != null) {
            //this.roleName = "ROLE_".concat(roleName.toUpperCase());
            this.roleName = roleName.toUpperCase();
        }
    }

}
