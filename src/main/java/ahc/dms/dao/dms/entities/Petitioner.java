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
@Table(name = "petitioner_details")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Petitioner {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "petitionerdetails_seq")
    @SequenceGenerator(name = "petitionerdetails_seq", sequenceName = "petitionerdetails_seq", allocationSize = 1)
    @Column(name = "pt_id")
    private Long pt_id;

    @Column(name = "pt_fd_mid")
    private Long pt_fd_mid;

    @Column(name = "pt_name")
    private String pt_name;

    @Column(name = "pt_email")
    private String pt_email;

    @Column(name = "pt_mobile")
    private Long pt_mobile;

    @Column(name = "pt_address")
    private String pt_address;

    @Column(name = "pt_pincode")
    private Long pt_pincode;

    @Column(name = "pt_other_contact")
    private String pt_other_contact;

    @Column(name = "pt_cr_by")
    private Long pt_cr_by;

    @Column(name = "pt_cr_date")
    private Date pt_cr_date;

    @Column(name = "pt_mod_by")
    private Long pt_mod_by;

    @Column(name = "pt_mod_date")
    private Date pt_mod_date;

    @Column(name = "pt_rec_status")
    private Integer pt_rec_status;

    @Column(name = "pt_sequence")
    private Integer pt_sequence;

    @Column(name = "pt_s_mid")
    private Long pt_s_mid;

    @Column(name = "pt_city")
    private String pt_city;
}
