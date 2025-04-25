package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "roles")
@Getter
@Setter
public class Role {

    @Id
    @Column(name = "role_id")
    private int roleId;
    @Column(name = "role_name", unique = true, nullable = false)
    private String roleName;
    @Column(nullable = false)
    private boolean status = true;

    @PrePersist
    @PreUpdate
    public void capitalizeRoleName() {
        if (this.roleName != null) {
            //this.roleName = "ROLE_".concat(roleName.toUpperCase());
            this.roleName = roleName.toUpperCase();
        }
    }

}
