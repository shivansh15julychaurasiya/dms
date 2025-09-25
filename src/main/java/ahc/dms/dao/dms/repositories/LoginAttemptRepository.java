package ahc.dms.dao.dms.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import ahc.dms.dao.dms.entities.LoginAttempt;

public interface LoginAttemptRepository extends JpaRepository<LoginAttempt, String> {
}