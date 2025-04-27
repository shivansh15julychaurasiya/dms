package ahc.dms.dao.pgdms.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "roles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class Role {

    @Id
    @Column(name = "role_id")
    private Integer roleId;
    @Column(name = "role_name", unique = true, nullable = false)
    private String roleName;
    @Column(nullable = false)
    private Boolean status = true;

    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

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

    // Getter, Setter and Constructors


    public Role(Integer roleId, String roleName, Boolean status) {
        this.roleId = roleId;
        this.roleName = roleName;
        this.status = status;
    }
}
