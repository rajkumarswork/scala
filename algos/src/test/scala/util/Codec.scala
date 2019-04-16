package rajkumars.info.util

import org.scalatest.FlatSpec

class CodecTest extends FlatSpec {

  "Codec" should "decode correctly" in {
    val ts =
      """OYO6Q9Q6%2FkISJuVpiD3PIh7ol2JLwcdup4cjh4cGMJ6Ec%2BmAI86ZHQcAICQ%2BJ%2Fs3LQIKPL9YrARjCulebrt2kDcp472l1vE5khYhMJgHEoRG8jXxx3bQ1%2FfqwUczLDzV8myxpEPJabkGfpAZgBrkQb3ZrJCPx%2Fm4opiuQ3cSneQOzxBjtCXRfbvjSSXrJCOOQVAlC%2FQXeslRBUjawFockQDEoQkiBbM7l%2BzPD4mcPUlf0hTqMiIJBrRE%2Bp7c3mue7KLQvzitynQRofOX4H8g48qVHCWhlqqJpCsFhnGkS48rcer4zDTa8n%2B2U6aeU76zjWz%2BVVIYvFaNz4vkHGs13uAexARSPGMtQL9mQNgZaC2oN2gCS7DMfwymX5H5jrXE3Mgwmw3DF3zW1mIPHqKASVza79icAeUviVnYUtPs9R%2BkYRnUVZbAMfeUieNQfQSQ3l%2FyZL3jl48gsrf5vdkShhYXVSCmHrmRxTT%2BL0lbz9Cg%2B%2Bfhs5slQiC3vIZBlmFA2m0pqvuCxAdXCMeFjt217b7WD9Nhx3pzBtP19e8BuMo%3D"""

    val res =
      """&algoid=SearchProductAd%3a628&bm_cbr=1&bm_d=&bm_tolevel=&cid=172&creative=790x260_T-C-OG_TI_1-4_AboveGrid1&fph=0&hlpt=S&ic=core&imp_s=0&json=hl_c8571914&loc=hl_1_999&maxads=3&pagetype=search&pc_pa=0&pc_pk=0&pgn=1&pcount=24&platform=web&ppt=Search&puserid=142754730.1646428157.1443567247&rt=S&p_searchterms=spatula&skuid=&sp_cp=0&sp_cpv=4&tax=undefined&v=4.5.1607&view=grid&uid=c7f7cd73-63cf-4396-bad1-376cc8961448&enc=1"""

    val dts = Codec.urldecode(ts)
    val dects = Codec.decrypt(dts)
    val dec = Codec.decode(dects)
    val unc = Zip.decompress(dec)
    assert(res === unc)
  }
}
