package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "order_from_elegalix")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrderFromElegalix {

    @Id
    private Long judgmentID;

    private String caseTypeCode;
    private String caseNumber;
    private Integer caseYear;

    @Temporal(TemporalType.DATE)
    private Date judgmentDate;

    private String highCourtBenchCode;
    private String judgmentType;
}
