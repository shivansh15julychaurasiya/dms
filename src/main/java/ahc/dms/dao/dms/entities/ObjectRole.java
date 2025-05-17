package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "object_role")
@Getter
@Setter
@NoArgsConstructor
public class ObjectRole {

    @Id
    @Column(name = "or_id")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "or_seq")
    @SequenceGenerator(
            name = "or_seq",
            sequenceName = "or_sequence", // This is the name of the DB sequence
            allocationSize = 1 // Optional: 1 means no batch caching
    )
    private Long orId;

    @Column(name = "status", nullable = false, columnDefinition = "boolean default true")
    private Boolean status;

    @ManyToOne(
            optional = false,
            fetch = FetchType.EAGER
    )
    @JoinColumn(name = "om_id")
    private ObjectMaster objectMaster;

    @ManyToOne(
            optional = false,
            fetch = FetchType.EAGER
    )
    @JoinColumn(name = "role_id")
    private Role role;

    public ObjectRole(ObjectMaster objectMaster, Role role, Boolean status) {
        this.objectMaster = objectMaster;
        this.role = role;
        this.status = status;
    }
}
