package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import org.hibernate.annotations.DynamicUpdate;

@Entity
@DynamicUpdate
@Table(name = "respondent_details")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Respondent {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "respondentdetails_seq")
    @SequenceGenerator(name = "respondentdetails_seq", sequenceName = "respondentdetails_seq", allocationSize = 1)
    @Column(name = "rt_id")
    private Long rt_id;

    @Column(name = "rt_fd_mid")
    private Long rt_fd_mid;

    @Column(name = "rt_name")
    private String rt_name;

    @Column(name = "rt_email")
    private String rt_email;

    @Column(name = "rt_mobile")
    private Long rt_mobile;

    @Column(name = "rt_address")
    private String rt_address;

    @Column(name = "rt_pincode")
    private Long rt_pincode;

    @Column(name = "rt_other_contact")
    private String rt_other_contact;

    @Column(name = "rt_cr_by")
    private Long rt_cr_by;

    @Column(name = "rt_cr_date")
    private Date rt_cr_date;

    @Column(name = "rt_mod_by")
    private Long rt_mod_by;

    @Column(name = "rt_mod_date")
    private Date rt_mod_date;

    @Column(name = "rt_rec_status")
    private Integer rt_rec_status;

    @Column(name = "rt_sequence")
    private Integer rt_sequence;

    @Column(name = "rt_s_mid")
    private Long rt_s_mid;

    @Column(name = "rt_city")
    private String rt_city;
}

