package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(
        name = "user_role",
        uniqueConstraints = @UniqueConstraint(columnNames = {"user_id", "role_id"})
)
@Getter
@Setter
public class UserRole {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_role_seq")
    @SequenceGenerator(
            name = "user_role_seq",
            sequenceName = "user_role_sequence", // This is the name of the DB sequence
            allocationSize = 1 // Optional: 1 means no batch caching
    )
    @Column(name = "ur_id")
    private Long userRoleId;

    @ManyToOne
    @JoinColumn(name = "user_id")
    private User userId;

    @ManyToOne
    @JoinColumn(name = "role_id")
    private Role role;

    @Column(name = "status", columnDefinition = "boolean default true")
    private boolean status = true;


}
