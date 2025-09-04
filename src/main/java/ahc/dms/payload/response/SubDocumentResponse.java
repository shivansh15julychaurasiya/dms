package ahc.dms.payload.response;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class SubDocumentResponse {
	
//  ********************** Fullstack Java Developer Vijay Chaurasiya *******************************

	
    private Long id;
    private String documentName;
    private Integer version;
    private Integer documentNo;
    private Integer documentYear;
    private String party;
    private String description;
    private LocalDateTime submittedDate;
    private Boolean nonMaintainable;
    private Boolean checked;
    private String applicationStatus;
    private Boolean pdfHighlightMode;
}

