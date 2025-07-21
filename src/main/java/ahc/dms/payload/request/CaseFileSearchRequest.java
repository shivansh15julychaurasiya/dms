package ahc.dms.payload.request;

import lombok.Data;

@Data
public class CaseFileSearchRequest {
    private Long caseTypeId;
    private String caseNo;
    private Integer caseYear;
}
