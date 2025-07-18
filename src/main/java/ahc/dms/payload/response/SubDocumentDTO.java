package ahc.dms.payload.response;

import lombok.Data;

@Data
public class SubDocumentDTO {
    private String documentName;
    private String description;
    private String party;
    private String documentUrl; // Assuming you store or generate URL
}
