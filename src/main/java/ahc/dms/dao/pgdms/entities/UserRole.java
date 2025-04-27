package ahc.dms.dao.pgdms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(
        name = "user_role",
        uniqueConstraints = @UniqueConstraint(columnNames = {"user_id", "role_id"})
)
@Getter
@Setter
@NoArgsConstructor
public class UserRole {

    @Version  // ← Optimistic lock column
    private Long version = 0L;

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_role_seq")
    @SequenceGenerator(
            name = "user_role_seq",
            sequenceName = "user_role_sequence", // This is the name of the DB sequence
            allocationSize = 1 // Optional: 1 means no batch caching
    )
    @Column(name = "ur_id")
    private Long urId;

    @ManyToOne(
            optional = false,
            fetch = FetchType.EAGER
    )
    @JoinColumn(name = "user_id")
    private User user;

    @ManyToOne(
            optional = false,
            fetch = FetchType.EAGER
    )
    @JoinColumn(name = "role_id")
    private Role role;

    @Column(name = "status", columnDefinition = "boolean default true")
    private Boolean status = true;


    public UserRole(User user, Role role, boolean status) {
        this.user = user;
        this.role = role;
        this.status = status;
    }
    
}
