package ahc.dms.dao.entities;

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
    private int roleId;
    @Column(name = "role_name", unique = true, nullable = false)
    private String roleName;
    @Column(nullable = false)
    private boolean status;

    @PrePersist
    public void handleData() {
        if (this.roleName != null) {
            //this.roleName = "ROLE_".concat(roleName.toUpperCase());
            this.status = true;
            this.roleName = roleName.toUpperCase();
        }
    }

    @PreUpdate
    public void capitalizeRoleName() {
        if (this.roleName != null) {
            //this.roleName = "ROLE_".concat(roleName.toUpperCase());
            this.roleName = roleName.toUpperCase();
        }
    }

}
