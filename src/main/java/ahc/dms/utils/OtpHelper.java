package ahc.dms.utils;

import ahc.dms.payload.OtpDto;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.util.UriBuilder;

import java.net.URI;

@Component
public class OtpHelper {

    public OtpDto generateLoginOtp(String phone){
        RestTemplate restTemplate = new RestTemplate();
        //URI uri
        return null;
    }

}
