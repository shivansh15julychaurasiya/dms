package ahc.dms.payload.response;

import lombok.Data;

import java.util.List;

@Data
public class CaseFileResponseDTO {
    private Long id;
    private String fdCaseNo;
    private Integer fdCaseYear;
    private String caseTypeName;
    private String fdFirstPetitioner;
    private String fdFirstRespondent;
    private List<SubDocumentDTO> subDocuments;
}
