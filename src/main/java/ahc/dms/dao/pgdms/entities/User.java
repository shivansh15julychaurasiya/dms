package ahc.dms.dao.pgdms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Entity
@Table(name = "users")
@Getter
@Setter
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class User implements UserDetails {

    @Version  // ← Optimistic lock column
    private Long version = 0L;

    @Id
    @Column(name = "user_id")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_seq")
    @SequenceGenerator(
            name = "user_seq",
            sequenceName = "user_sequence", // This is the name of the DB sequence
            allocationSize = 1 // Optional: 1 means no batch caching
    )
    private Long userId;

    @Column(name = "login_id", nullable = false, length=100, unique = true)
    private String loginId;
    @Column(name = "name", nullable = false, length=100)
    private String name;
    @Column(name = "email", nullable = false, length=100, unique = true)
    private String email;
    @Column(name = "phone", nullable = false, length=100, unique = true)
    private String phone;
    @Column(name = "password", nullable = false, length=100)
    private String password;
    private String about;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;
    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    @CreatedBy
    @Column(name = "created_by", updatable = false)
    private String createdBy;
    @LastModifiedBy
    @Column(name = "updated_by")
    private String updatedBy;

    @OneToMany(
            mappedBy = "user",
            orphanRemoval = true,
            fetch = FetchType.EAGER
    )
    private Set<UserRole> userRoles = new HashSet<>();

    // Utility method to add roles
    public void addRole(Role role, boolean status) {
        UserRole userRole = new UserRole(this, role, status);
        userRoles.add(userRole);
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {

        // finding active roles in user-role as well as role (master) entities
        Set<UserRole> activeUserRoles = new HashSet<>();
        for (UserRole eachUserRole :  userRoles) {
            if (eachUserRole.getStatus() && eachUserRole.getRole().getStatus()) {
                activeUserRoles.add(eachUserRole);
            }
        }

        /*
        List<SimpleGrantedAuthority> authorities = this.userRoles.stream()
                .map((userRole) -> new SimpleGrantedAuthority(userRole.getRole().getRoleName()))
                .collect(Collectors.toList());

         */
        // setting only those roles which are active at user-role and global (role) entity level
        List<SimpleGrantedAuthority> authorities = activeUserRoles
                .stream()
                .map(activeUserRole -> new SimpleGrantedAuthority(activeUserRole.getRole().getRoleName()))
                .collect(Collectors.toList());

        return authorities;
    }

    @Override
    public String getUsername() {
        return this.loginId;
    }

    @Override
    public boolean isAccountNonExpired() {
        return UserDetails.super.isAccountNonExpired();
    }

    @Override
    public boolean isAccountNonLocked() {
        return UserDetails.super.isAccountNonLocked();
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return UserDetails.super.isCredentialsNonExpired();
    }

    @Override
    public boolean isEnabled() {
        return UserDetails.super.isEnabled();
    }
}
