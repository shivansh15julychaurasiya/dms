package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "object_master")
@Getter
@Setter
@NoArgsConstructor
public class ObjectMaster {

    @Id
    @Column(name = "om_id")
    private Long omId;

    @Column(name = "request_uri")
    private String requestUri;
    @Column(name = "request_method")
    private String requestMethod;
    @Column(name = "status", nullable = false, columnDefinition = "boolean default true")
    private Boolean status;

    @OneToMany(
            mappedBy = "objectMaster",
            orphanRemoval = true,
            fetch = FetchType.EAGER
    )
    private Set<ObjectRole> objectRoles = new HashSet<>();

}