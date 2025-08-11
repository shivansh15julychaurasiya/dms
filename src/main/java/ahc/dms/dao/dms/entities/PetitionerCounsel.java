package ahc.dms.dao.dms.entities;



import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;


import java.util.Date;

@Entity
@Table(name = "petitioner_counsel")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PetitionerCounsel {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "pc_seq")
    @SequenceGenerator(name = "pc_seq", sequenceName = "pc_seq", allocationSize = 1)
    @Column(name = "pc_id")
    private Long id;

    @Column(name = "pc_name")
    private String name;

    @Column(name = "pc_rec_status")
    private Integer recStatus;

    @Column(name = "pc_fd_mid")
    private Long fdMid;

    @Column(name = "pc_sequence")
    private Integer sequence;

    @Column(name = "pc_cr_by")
    private Long createdBy;

    @Column(name = "pc_cr_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date createdDate;

    @Column(name = "pc_mod_by")
    private Long modifiedBy;

    @Column(name = "pc_mod_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date modifiedDate;
}
