package ahc.dms.payload.request;

import lombok.Data;
import java.util.Date;

@Data
public class CauseListSearchRequest {
    private String courtName;
    private String causeListDesc;
    private Date date;
}
