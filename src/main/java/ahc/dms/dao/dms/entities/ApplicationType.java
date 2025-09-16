package ahc.dms.dao.dms.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;

@Entity
@Table(name = "application_types")

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

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Long getParent() {
        return parent;
    }

    public void setParent(Long parent) {
        this.parent = parent;
    }

    public Integer getRecStatus() {
        return recStatus;
    }

    public void setRecStatus(Integer recStatus) {
        this.recStatus = recStatus;
    }

    public Integer getSequence() {
        return sequence;
    }

    public void setSequence(Integer sequence) {
        this.sequence = sequence;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public Integer getMapping() {
        return mapping;
    }

    public void setMapping(Integer mapping) {
        this.mapping = mapping;
    }
}