package ahc.dms;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.pgdms.entities.Role;
import ahc.dms.dao.pgdms.entities.User;
import ahc.dms.dao.pgdms.entities.UserRole;
import ahc.dms.dao.pgdms.repositories.RoleRepository;
import ahc.dms.dao.pgdms.repositories.UserRepository;
import ahc.dms.dao.pgdms.repositories.UserRoleRepository;
import ahc.dms.payload.UserRoleDto;
import org.modelmapper.ModelMapper;
import org.modelmapper.config.Configuration;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.data.jpa.JpaRepositoriesAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceTransactionManagerAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Stream;

@SpringBootApplication(exclude = {
        DataSourceAutoConfiguration.class, // Prevents default single DataSource auto-configuration
        HibernateJpaAutoConfiguration.class, // Stops Hibernate from autoconfiguring based on a single DataSource
        JpaRepositoriesAutoConfiguration.class, // Disables automatic JPA repository scanning
        DataSourceTransactionManagerAutoConfiguration.class // Avoids auto-creation of a default transaction manager
})
//public class DmsApplication extends SpringBootServletInitializer implements CommandLineRunner{
public class DmsApplication implements CommandLineRunner {

    @Autowired
    private PasswordEncoder passwordEncoder;
    @Autowired
    private RoleRepository roleRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private UserRoleRepository userRoleRepository;

    public static void main(String[] args) {
        SpringApplication.run(DmsApplication.class, args);
    }

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();

		// Configure global settings
		modelMapper.getConfiguration()
				.setMatchingStrategy(MatchingStrategies.STANDARD) // Use STRICT for exact matching
				.setAmbiguityIgnored(true) // ignore ambiguous matches
				.setFieldAccessLevel(Configuration.AccessLevel.PRIVATE);

		// Explicit UserRole to UserRoleDto mapping
		modelMapper.typeMap(UserRole.class, UserRoleDto.class)
				.addMappings(mapper -> {
					// Explicitly define all mappings
					//mapper.map(src -> src.getUrId(), UserRoleDto::setUrId);
					mapper.map(src -> src.getUser().getUserId(), UserRoleDto::setUserId);
					mapper.map(src -> src.getRole().getRoleId(), UserRoleDto::setRoleId);
					//mapper.map(src -> src.isStatus(), UserRoleDto::setStatus);
				});

		// Validate the configuration
		modelMapper.validate();
		return  modelMapper;
    }

    @Override
    @Transactional(transactionManager = "pgDmsTransactionManager")
    public void run(String... args) {

        try {
            // Create and save roles once
            List<Role> roles = List.of(
                    new Role(AppConstants.ADMIN_USER, "ROLE_ADMIN", true),
                    new Role(AppConstants.NORMAL_USER, "ROLE_USER", true),
                    new Role(AppConstants.ECOURT_USER, "ROLE_ECOURT", true)
            );
            List<Role> savedRoles = roleRepository.saveAll(roles);

            // Get references to saved roles
            Role adminRole = savedRoles.get(0);
            Role userRole = savedRoles.get(1);
            Role ecourtRole = savedRoles.get(2);

            User firstUser = userRepository.findByLoginId("11448")
                    .orElseGet(() -> {
                        User newUser = new User();
                        newUser.setName("amit");
                        newUser.setLoginId("11448");
                        newUser.setEmail("amitvarmaone@gmail.com");
                        newUser.setAbout("admin");
                        newUser.setPhone("8601837554");
                        newUser.setPassword(passwordEncoder.encode("1234"));
                        return userRepository.saveAndFlush(newUser);
                    });

            // For each role, check if it exists first
            Stream.of(adminRole, userRole, ecourtRole)
                    .forEach(role -> {
                        if (!userRoleRepository.existsByUserAndRole(firstUser, role)) {
                            userRoleRepository.save(new UserRole(firstUser, role, true));
                        }
                    });


            System.out.println("Admin User created");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
