package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "comments")
@NoArgsConstructor
@Getter
@Setter
public class Comment {

    @Id
    @Column(name = "comment_id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int commentId;

    private String content;

    @ManyToOne
    @JoinColumn(name = "post_mid")
    private Post post;

    @ManyToOne
    @JoinColumn(name = "user_mid")
    private User user;

}
