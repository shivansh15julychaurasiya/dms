package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Data;

import java.util.List;

@Entity
@Table(name = "case_types")
@Data
public class CaseType {
    @Id
    @Column(name = "ct_id")
    private Long id;

    @Column(name = "ct_label")
    private String label;

    @Column(name = "ct_name")
    private String name;

    @Column(name = "ct_bench_code")
    private Long benchCode;

    @Column(name = "ct_lk_mid")
    private Long lookupId;

    @Column(name = "ct_status")
    private Short status;




}