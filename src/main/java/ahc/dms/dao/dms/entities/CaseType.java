package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Getter
@Setter
@Table(name = "case_types")
public class CaseType {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name="ct_id")
    private Long ct_id;

    @Column(name="ct_label")
    private String ct_label;

    @Column(name="ct_name")
    private String ct_name;

    @Column(name = "ct_bench_code")
    private Long ct_bench_code;

    @Column(name = "ct_lk_mid")
    private Long ct_lk_mid;

    @Column(name = "ct_status")
    private Integer ct_status;



}
