export default function Nodata({ message = 'No Data' }: { message?: string }) {
  return (
    <div className="d-flex my-4 justify-content-center text-secondary fst-italic">
      {message}
    </div>
  );
}
