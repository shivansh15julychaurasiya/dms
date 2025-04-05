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

    private String name;

}
