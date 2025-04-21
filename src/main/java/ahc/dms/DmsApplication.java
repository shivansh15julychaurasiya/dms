package ahc.dms;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.entities.Role;
import ahc.dms.dao.respositories.RoleRepository;
import org.modelmapper.ModelMapper;
import org.modelmapper.config.Configuration;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.support.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;

@SpringBootApplication
public class DmsApplication extends SpringBootServletInitializer implements CommandLineRunner{

	@Autowired
	private PasswordEncoder passwordEncoder;
	@Autowired
	private RoleRepository roleRepository;

	public static void main(String[] args) {
		SpringApplication.run(DmsApplication.class, args);
	}

	@Bean
	public ModelMapper modelMapper() {
		ModelMapper mapper = new ModelMapper();

		mapper.getConfiguration()
				.setFieldMatchingEnabled(true)
				.setFieldAccessLevel(Configuration.AccessLevel.PRIVATE)
				.setMatchingStrategy(MatchingStrategies.LOOSE);

		return mapper;
	}

	@Override
	public void run(String... args) {
		System.out.println(this.passwordEncoder.encode("1234"));
		try {
			Role adminRole = new Role();
			adminRole.setRoleId(AppConstants.ADMIN_USER);
			adminRole.setName("ROLE_ADMIN");

			Role userRole = new Role();
			userRole.setRoleId(AppConstants.NORMAL_USER);
			userRole.setName("ROLE_USER");

			List<Role> roles = List.of(adminRole, userRole);
			List<Role> savedRoles = roleRepository.saveAll(roles);
			savedRoles.forEach(r -> System.out.println(r.getName()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
