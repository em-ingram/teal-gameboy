import styles from '../styles/Home.module.css'
import { CPU } from '../gameboy-emu/src/cpu';
import { MMU } from '../gameboy-emu/src/mmu';

export default function Home() {

  return (
    <div className={styles.container}>
      hello gameboy emu
    </div>
  )
}
