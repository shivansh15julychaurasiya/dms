package ahc.dms.dao.pgdms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Date;

@Entity
@Table(name = "token")
@Getter
@Setter
@NoArgsConstructor
public class Token {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "token_seq")
    @SequenceGenerator(
            name = "token_seq",
            sequenceName = "token_sequence", // DB sequence name
            allocationSize = 1
    )
    @Column(name = "token_id")
    private Long tokenId;



    @Column(name = "login_id", nullable = false)
    private String loginId;

    private String jwtToken;
    private Date expirationDate;
    private boolean tokenStatus;

}
