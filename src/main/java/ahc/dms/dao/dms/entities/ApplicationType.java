package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Table(name = "application_types")
@Data
public class ApplicationType {
    @Id
    @Column(name = "at_id")
    private Integer id;

    @Column(name = "at_type")
    private String type;

    @Column(name = "at_name")
    private String name;

    @Column(name = "at_description")
    private String description;

    @Column(name = "at_parent")
    private Long parent;

    @Column(name = "at_rec_status")
    private Integer recStatus;

    @Column(name = "at_sequence")
    private Integer sequence;

    @Column(name = "at_code")
    private Integer code;

    @Column(name = "at_ald_lko_mapping")
    private Integer mapping;
}