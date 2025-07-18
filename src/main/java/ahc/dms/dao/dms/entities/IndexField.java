package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

import java.time.LocalDateTime;

@Entity
@Table(name = "index_fields")
@Data
public class IndexField {
    @Id
    @Column(name = "if_id")
    private Long id;

    @Column(name = "if_name")
    private String name;

    @Column(name = "if_label")
    private String label;

    @Column(name = "if_type")
    private String type;

    @Column(name = "if_cr_by")
    private Long createdBy;

    @Column(name = "if_cr_date")
    private LocalDateTime createdDate;

    @Column(name = "if_mod_by")
    private Long modifiedBy;

    @Column(name = "if_mod_date")
    private LocalDateTime modifiedDate;

    @Column(name = "if_rec_status")
    private Integer recStatus;

    @Column(name = "if_required_status")
    private Integer requiredStatus;

    @Column(name = "if_add_multiple")
    private Long addMultiple;

    @Column(name = "if_parent")
    private Long parent;

    @Column(name = "if_sequence")
    private Integer sequence;

    @Column(name = "if_level")
    private Long level;

    @Column(name = "if_type_code")
    private String typeCode;

    @Column(name = "if_extr_info")
    private String extraInfo;
}
