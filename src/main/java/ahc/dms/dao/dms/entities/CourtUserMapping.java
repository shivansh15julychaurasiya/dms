package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Data;


@Entity
@Table(name = "court_user_mapping")
@Data
public class CourtUserMapping {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "court_user_mapping_seq")
    @SequenceGenerator(name = "court_user_mapping_seq",sequenceName = "court_user_mapping_seq",allocationSize = 1)
    @Column(name = "cum_id")
    private Long CumId;

    @Column(name = "cum_court_mid")
    private Integer CumCourtMid;

    @Column(name = "cum_user_mid")
    private Long CumUserMid;

    @Column(name = "cum_jg_mid")
    private Long CumJgMid;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cum_court_mid",insertable = false,updatable = false)
    private CourtMaster courtMaster;

    @Override
    public String toString() {
        return "CourtUserMapping{" +
                "CumId=" + CumId +
                ", CumCourtMid=" + CumCourtMid +
                ", CumUserMid=" + CumUserMid +
                ", CumJgMid=" + CumJgMid +
                ", courtMaster=" + courtMaster +
                '}';
    }
}
